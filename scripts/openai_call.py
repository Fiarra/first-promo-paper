# scripts/openai_call.py

import os
from dotenv import load_dotenv
from openai import OpenAI

# Load .env for OPENAI_API_KEY
load_dotenv()

# Instantiate the client with your key
client = OpenAI(
    api_key=os.getenv("OPENAI_API_KEY"),
)

def get_response(system_content, user_content, model="gpt-4o", response_format=None):
    """
    A minimal function that calls the Chat Completions endpoint
    and returns the model's response text.

    :param system_content: The system-level instruction or context
    :param user_content: The user prompt or user query
    :param model: The model name (default is 'gpt-4o')
    :param response_format: Custom parameter if your deployment supports it
                           (e.g., {"type": "json_object"}). If not, it can be None.
    :return: The raw text content returned by the model
    """

    chat_completion = client.chat.completions.create(
        model=model,
        messages=[
            {"role": "system", "content": system_content},
            {"role": "user", "content": user_content},
        ],
        response_format=response_format  # If your custom model uses it
    )

    response_message = chat_completion.choices[0].message.content
    return response_message

