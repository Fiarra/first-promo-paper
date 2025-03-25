# prompts/title_abstract_prompts.py

SYSTEM_PROMPT = """
You are an expert in analyzing academic texts.
"""

USER_PROMPT_TEMPLATE = """
You will receive text from a paper, which may contain a title and an abstract.
We want to extract these fields from the text.

Return ONLY valid JSON in the format below (and nothing else).
**Do not** include triple backticks, code blocks, or any additional markdown:

{{
  "title": "...",
  "abstract": "...",
  "debug_text": "..."
}}

If you cannot find a title or abstract, use "I don't know!!!" as that field's value.

Here is the text to analyze:
\"\"\"{text}\"\"\"
"""
