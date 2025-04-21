import os
import time
from dotenv import load_dotenv, find_dotenv
from pprint import pprint

load_dotenv()

from openai import OpenAI

client = OpenAI(
  api_key= os.environ['OPENAI_API_KEY'], 
)

assistant_id = os.environ['ASSISTANT_ID']

thread = client.beta.threads.create()

while True:
    user_input = input("User: ")
    
    if user_input.lower() == "exit":
        print("Exiting chat. Goodbye!")
        break

    # Add user message to thread
    client.beta.threads.messages.create(
        thread_id=thread.id,
        role="user",
        content=user_input
    )

    # Create a run for assistant response
    run = client.beta.threads.runs.create(
        thread_id=thread.id,
        assistant_id=assistant_id
    )

    # Poll for assistant response
    while True:
        _run = client.beta.threads.runs.retrieve(thread_id=thread.id, run_id=run.id)
        
        if _run.status == "completed":
            thread_messages = client.beta.threads.messages.list(thread.id)
            assistant_reply = thread_messages.data[0].content[0].text.value
            print(f"Assistant: {assistant_reply}\n")

            steps = client.beta.threads.runs.steps.list(thread_id=thread.id, run_id=_run.id)
            for step in steps.data:
                run_step = client.beta.threads.runs.steps.retrieve(
                thread_id=thread.id,
                run_id=_run.id,
                step_id=step.id,  
                include=["step_details.tool_calls[*].file_search.results[*].content"]
            )
            # if run_step.step_details and hasattr(run_step.step_details, "tool_calls"):
            #     i = 0
            #     for tool_call in run_step.step_details.tool_calls:
            #         if tool_call.type == "file_search":
            #             for result in tool_call.file_search.results:
            #                 while i < 3:
            #                     i+=1 
            #                     print(f"Found relevant content:\n{result.content}")
            break
        time.sleep(1)  # Wait before polling again

#implemement feature that allows users to upload files and add that to the thread too
