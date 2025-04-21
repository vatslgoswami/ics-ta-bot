require('dotenv').config();
const express = require('express');
const session = require('express-session');
const { OpenAI } = require('openai');

const app = express();
const PORT = 3000;

const openai = new OpenAI({ apiKey: process.env.OPENAI_API_KEY });
const assistant_id = process.env.ASSISTANT_ID;

// Middleware
app.use(express.static('public'));
app.use(express.json());
app.use(session({
  secret: 'keyboard cat', // Replace with a secure secret in production
  resave: false,
  saveUninitialized: true,
  cookie: { secure: false } // true if using HTTPS
}));

app.post('/api/query', async (req, res) => {
  try {
    // Get thread from session, or create a new one
    if (!req.session.threadId) {
      const thread = await openai.beta.threads.create();
      req.session.threadId = thread.id;
    }

    const threadId = req.session.threadId;
    const prompt = req.body.prompt;

    // Add user message to thread
    await openai.beta.threads.messages.create(threadId, {
      role: "user",
      content: prompt,
    });

    // Run the assistant
    const run = await openai.beta.threads.runs.create(threadId, {
      assistant_id,
    });

    // Poll until run is complete
    let completed = false;
    let responseText = "No response.";
    while (!completed) {
      const status = await openai.beta.threads.runs.retrieve(threadId, run.id);
      if (status.status === "completed") {
        const messages = await openai.beta.threads.messages.list(threadId);
        const lastMessage = messages.data[0];
        responseText = lastMessage.content[0].text.value;
        completed = true;
      } else if (status.status === "failed") {
        throw new Error("Assistant failed to respond.");
      }
      await new Promise(resolve => setTimeout(resolve, 1000));
    }

    res.json({ message: responseText });

  } catch (error) {
    console.error('Error with Assistant API:', error.message);
    res.status(500).json({ error: 'Failed to fetch assistant response', details: error.message });
  }
});

app.post('/reset', (req, res) => {
    req.session.destroy(() => {
      res.json({ message: 'Session reset' });
    });
  });

app.listen(PORT, () => {
  console.log(`Server running at http://localhost:${PORT}`);
});
