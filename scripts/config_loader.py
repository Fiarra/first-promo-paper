"""
Configuration loader for the PDF processing pipeline.
Loads configuration from YAML files and provides access to the settings.
"""

import os
import yaml
from typing import Dict, Any, Optional


class ConfigLoader:
    """
    Loads and provides access to configuration settings from YAML files.
    Supports default configurations and profile-specific overrides.
    """
    
    def __init__(self, config_dir: str = "../configs"):
        """
        Initialize the configuration loader.
        
        Args:
            config_dir: Directory containing configuration files
        """
        self.config_dir = config_dir
        self.config = {}
        self.loaded_profile = None
        
        # Load default config first
        default_config_path = os.path.join(config_dir, "default.yaml")
        if os.path.exists(default_config_path):
            with open(default_config_path, "r") as f:
                self.config = yaml.safe_load(f) or {}
        
    def load_profile(self, profile_name: str) -> bool:
        """
        Load a profile-specific configuration, which overrides default settings.
        
        Args:
            profile_name: Name of the profile (without .yaml extension)
            
        Returns:
            True if profile was loaded successfully, False otherwise
        """
        profile_path = os.path.join(self.config_dir, f"{profile_name}.yaml")
        
        if not os.path.exists(profile_path):
            print(f"Warning: Profile '{profile_name}' not found at {profile_path}")
            return False
            
        with open(profile_path, "r") as f:
            profile_config = yaml.safe_load(f) or {}
            
        # Deep merge the profile config into the default config
        self._deep_update(self.config, profile_config)
        self.loaded_profile = profile_name
        return True
    
    def get(self, key_path: str, default: Any = None) -> Any:
        """
        Get a configuration value using dot notation.
        
        Args:
            key_path: Path to the config value (e.g., "llm.model")
            default: Default value if key not found
            
        Returns:
            The configuration value or default if not found
        """
        keys = key_path.split(".")
        value = self.config
        
        for key in keys:
            if isinstance(value, dict) and key in value:
                value = value[key]
            else:
                return default
                
        return value
    
    def get_all(self) -> Dict[str, Any]:
        """
        Get the entire configuration dictionary.
        
        Returns:
            The complete configuration dictionary
        """
        return self.config
    
    def _deep_update(self, base_dict: Dict[str, Any], update_dict: Dict[str, Any]) -> None:
        """
        Recursively update a dictionary with another dictionary.
        
        Args:
            base_dict: Dictionary to update
            update_dict: Dictionary with new values
        """
        for key, value in update_dict.items():
            if isinstance(value, dict) and key in base_dict and isinstance(base_dict[key], dict):
                self._deep_update(base_dict[key], value)
            else:
                base_dict[key] = value


# Global instance for easy import in other modules
config = ConfigLoader()

# Helper functions for common use cases
def load_config(profile: Optional[str] = None) -> ConfigLoader:
    """
    Load configuration with optional profile.
    
    Args:
        profile: Optional profile name to load
        
    Returns:
        The ConfigLoader instance
    """
    if profile:
        config.load_profile(profile)
    return config 