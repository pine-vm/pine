# How to Build a Backend App in Elm

To support building Elm backend apps, Pine brings the following components:

+ Web service platform as a type of Elm application. Enables handling HTTP requests from clients.
+ Database management system: Guarantees state changes in the backend Elm app are [ACID](https://en.wikipedia.org/wiki/ACID), and deployments are type-safe.
+ Volatile processes: A generic interface for hosting third-party components in containers and integrating them with the Elm app, similar to the 'ports' in frontend Elm apps.

Pine integrates and manages these aspects automatically to avoid distractions and lets us focus on business logic.
