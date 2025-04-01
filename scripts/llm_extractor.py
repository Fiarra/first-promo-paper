import os
import json
from dotenv import load_dotenv
from scripts.config_loader import config

# Load environment for OPENAI_API_KEY, if needed
load_dotenv()

# Import our basic API call function
from scripts.openai_call import get_response

# Import the system prompt and user prompt template
from prompts.title_abstract_prompts import SYSTEM_PROMPT, USER_PROMPT_TEMPLATE

def extract_title_abstract_with_llm(text, model=None):
    """
    Extract title and abstract from text using LLM.
    
    Args:
        text: Text containing title and abstract to extract
        model: LLM model to use (default from config)
        
    Returns:
        Tuple of (title, abstract)
    """
    # Use model from arguments or fall back to config
    if model is None:
        model = config.get("llm.model", "gpt-4o-mini")
    
    # Get max token length from config
    max_tokens = config.get("llm.max_input_tokens", 3000)
    truncated_text = text[:max_tokens]

    # Debug (Python side):
    # print("DEBUG: Partial text snippet:\n", truncated_text[:300], "\n")  # Show first 300 chars

    user_prompt = USER_PROMPT_TEMPLATE.format(text=truncated_text)

    # Get max retries from config
    max_retries = config.get("processing.error_handling.max_retries", 3)
    retry_delay = config.get("processing.error_handling.retry_delay", 5)
    
    response_text = None
    attempts = 0
    
    # Try to get a response, with retries
    while response_text is None and attempts < max_retries:
        try:
            response_text = get_response(
                system_content=SYSTEM_PROMPT,
                user_content=user_prompt,
                model=model
            )
            break
        except Exception as e:
            attempts += 1
            print(f"Error calling LLM (attempt {attempts}/{max_retries}): {str(e)}")
            
            # If we have a fallback model and this is our last attempt, try the fallback
            if attempts == max_retries - 1 and config.get("llm.fallback_model"):
                fallback_model = config.get("llm.fallback_model")
                print(f"Trying fallback model: {fallback_model}")
                model = fallback_model
            
            import time
            time.sleep(retry_delay)
    
    # If we still have no response after all retries
    if response_text is None:
        return "Error: Failed to extract title", "Error: Failed to extract abstract"

    try:
        parsed = json.loads(response_text)
        
        title = parsed.get("title", "").strip()
        abstract = parsed.get("abstract", "").strip()
        debug_text = parsed.get("debug_text", "").strip()
        
        return title, abstract
    
    except json.JSONDecodeError:
        print(f"Failed to parse LLM response as JSON: {response_text[:100]}...")
        return "Error: JSON parse error", "Error: JSON parse error"