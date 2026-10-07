# Security and verification boundaries

Do not disclose API keys in issues, logs, screenshots or pull requests. Report suspected credential exposure privately to the repository owner through their portfolio contact channel. Rotate exposed credentials; deleting a file from the latest commit does not remove it from Git history.

Provider credentials are read on the server. Shared work uses bounded queues, deduplication and global/endpoint retry backoff. History requests enforce supported date boundaries. Public refresh/retry actions must not reset shared backoff.

The owner supplied successful output for 26 focused offline, mocked-function regression checks covering backoff, attempt spacing, history capacity and date validation. These checks did not exercise real transport, provider workers, application startup or production infrastructure. They do not establish that the app is exploit-free or that a comprehensive penetration test passed.

Remaining considerations include distributed quota controls, deployment secrets, dependency maintenance, cache retention, analytics privacy and full integration testing. Do not run load or penetration tests against third-party hosting infrastructure without its authorization.
