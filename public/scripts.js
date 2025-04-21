document.addEventListener('DOMContentLoaded', function() {
    const askButton = document.getElementById('askButton');
    const questionInput = document.getElementById('questionInput');
    const chatContainer = document.getElementById('chatContainer');

    askButton.addEventListener('click', function() {
        const prompt = questionInput.value.trim();
        if (!prompt) return;

        appendMessage("you", prompt);
        questionInput.value = "";

        questionInput.blur();
        askButton.blur();

        fetch('/api/query', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ prompt: prompt })
        })
        .then(response => {
            if (!response.ok) {
                throw new Error('Network response was not ok: ' + response.statusText);
            }
            return response.json();
        })
        .then(data => {
            console.log('Received data:', data);
            appendMessage("assistant", data.message);
        })
        .catch(error => {
            console.error('Error:', error);
            appendMessage("assistant", 'Failed to load response. ' + error.message);
        });
    });

    fetch('/reset', { method: 'POST' });

    questionInput.addEventListener('keypress', function(e) {
        if (e.key === 'Enter') {
            askButton.click();
        }
    });
    
    function appendMessage(role, text) {
        const chatContainer = document.getElementById("chatContainer");
    
        const messageDiv = document.createElement("div");
        messageDiv.classList.add("message", role === "you" ? "user" : "bot");
    
        const roleSpan = document.createElement("span");
        roleSpan.classList.add("role");
        roleSpan.textContent = role;
    
        messageDiv.appendChild(roleSpan);
    
        const content = document.createElement("div");
        content.innerHTML = role === "assistant" ? marked.parse(text) : text;
    
        messageDiv.appendChild(content);
        chatContainer.appendChild(messageDiv);
        chatContainer.scrollTop = chatContainer.scrollHeight;
    }
});
