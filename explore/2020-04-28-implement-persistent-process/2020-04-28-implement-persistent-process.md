# 2020-04-28 Implement Persistent Process

Recording some discoveries I made during the change in the implementation today. Just two days ago, before reading through the existing implementation again, I was not aware of these details, only understood it today. Figured since I needed to spend so much time up close before finding a satisfying solution for the new process store, better record this before I forget it again.

Before looking into implementation details, an overview closer to public interfaces and observable parts should help with navigating. With the addition of migrations, I arrived at these four kinds of events for the persistent history of the backend process:

+ Update for an event in the Elm app: This kind usually accounts for more than 99.9% of the events, handling clients HTTP requests and task results.
+ Set the state of the Elm app: Used for manual intervention, for example, when migrating the state of an app for which no copy of the process store is available.
+ Deploy app config and migrate Elm app state: Deploy a new configuration, including an Elm app for the backend. Uses the migrate function in the Elm code to migrate the state from the previously active Elm app.
+ Deploy app config and init Elm app state: Deploy a new configuration, including an Elm app for the backend. In contrast to the deployment with migration, this one uses the init function in the Elm code to initialize the state of the backend.

The implementation to execute Elm code uses a javascript engine. The instance of the javascript engine depends on volatile memory and is destroyed when losing the state of the volatile memory. With this setup, we need a process to restore the representation in volatile memory from the persistent records of the process history.

There are two implementations to advance the state of the backend process in the volatile representation:

+ The restore path: This is the reference and applied on host startup to restore the representation in volatile memory state from the persistent records. This implementation applies events from the process history, starting from the newest reduction it finds in the persistent store.
+ The Elm app event path: This path exists to reduce runtime expenses. A specialized implementation to save processing time. This path only supports one the update for an event in the scope of the current Elm app.

To keep the implementation simple, the other kinds of events possible in the backend process are applied via the restore path: Persist the event in the store and then use the same route as during host startup.

But what about failing migrations? We want to reject any attempt to deploy a new Elm app when the type for the previous Elm app state model in the migrate function does not match.
If there is an error in the new app or the migrate function, we do not want to persist this deployment but return an error message for the deployment attempt on the admin interface. The public app should continue as if that deployment had never been attempted.

The implementation projects the state of the store as it would be after appending the deployment event. This projected state of the persistent store is then used with the common restore function. Only if the migration of the Elm app state worked in this test, the deployment event is persisted to the main process history.

