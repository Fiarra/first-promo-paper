# scripts/openai_call.py

import os
from dotenv import load_dotenv
from openai import OpenAI
from scripts.config_loader import config

# Load .env for OPENAI_API_KEY
load_dotenv()

# Instantiate the client with your key
client = OpenAI(
    api_key=os.getenv("OPENAI_API_KEY"),
)

def get_response(system_content, user_content, model=None, response_format=None):
    """
    Call the Chat Completions endpoint and return the model's response.

    Args:
        system_content: The system-level instruction or context
        user_content: The user prompt or query
        model: The model name (default from config)
        response_format: Custom parameter if your deployment supports it
                       (e.g., {"type": "json_object"})
                       
    Returns:
        The raw text content returned by the model
    """
    # Use model from arguments or fall back to config
    if model is None:
        model = config.get("llm.model", "gpt-4o-mini")
    
    # Override with default response format if not provided
    if response_format is None and user_content and "json" in user_content.lower():
        response_format = {"type": "json_object"}

    try:
        chat_completion = client.chat.completions.create(
            model=model,
            messages=[
                {"role": "system", "content": system_content},
                {"role": "user", "content": user_content},
            ],
            response_format=response_format
        )

        response_message = chat_completion.choices[0].message.content
        return response_message
        
    except Exception as e:
        # Log the error but re-raise it to be handled by the caller
        print(f"Error calling OpenAI API with model {model}: {str(e)}")
        raise

