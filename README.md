# Elm Slack export viewer

Expects the Slack export in `/export/`, eg.

```
- export/
    |- users.json
    |- general
        |- 2021-01-01.json
```

Before running the Elm app run `node list-days.mjs` to generate `days.json` files inside the channels.

## TODO

- [ ] translate user IDs to usernames
- [ ] translate channel IDs to channel names, and link to them

