import os
import json
from dotenv import load_dotenv

# Load environment for OPENAI_API_KEY, if needed
load_dotenv()

# Import our new basic API call function
from scripts.openai_call import get_response

# Import the system prompt and user prompt template
from prompts.title_abstract_prompts import SYSTEM_PROMPT, USER_PROMPT_TEMPLATE

def extract_title_abstract_with_llm(text, model="gpt-4o"):
    """
    Truncates 'text' if needed, calls the get_response function,
    expects valid JSON with 'title' and 'abstract'.
    Returns (title, abstract).
    """
    truncated_text = text[:3000]

    # Debug (Python side):
    # print("DEBUG: Partial text snippet:\n", truncated_text[:300], "\n")  # Show first 300 chars

    user_prompt = USER_PROMPT_TEMPLATE.format(text=truncated_text)

    response_text = get_response(
        system_content=SYSTEM_PROMPT,
        user_content=user_prompt,
        model=model
    )

    #print("LLM RESPONSE:\n", response_text)  # Show raw LLM output

    parsed = json.loads(response_text)

    title = parsed.get("title", "").strip()
    abstract = parsed.get("abstract", "").strip()
    debug_text = parsed.get("debug_text", "").strip()
    #print("MODEL'S DEBUG TEXT (echo of first 50 chars):", debug_text)

    return title, abstract