-record(organization, {id,
                     name,
                     address,
                     plan
                   }).
-record(accounts, {name,
                  orgid,
                  email,
                  permissions=[],
                  password,
                  createdOn}).
-record(applications, {id,
                  name,
                  description,
                  apikeys=[],
                  ownedTopics=[],
                  subscribedTopics=[],
                  createdOn,
                  encrypt=[]}).
-record(tblschemas, {id,
                  validation,
                  version,
                  createdOn}).
-record(topics, {id,
                  name,
                  description,
                  schemaId,
                  createdOn}).
-record(dyntable, {pubId,
                  topicId,
                  schemaId,
                  payload,
                  createdOn}).
-record(counter_published, {name, value=0}).
-record(counter_consumed, {name, value=0}).
