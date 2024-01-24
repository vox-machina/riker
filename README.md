# Riker

An interface for recording what interests you in the world and for surfacing information that matters to you.

Part of the Nanonics architecture.

## Getting started

### Development

1. Copy the 'config.template.edn' file to 'config.edn' and configure values per your needs.
2. A convenience script 'run.sh' can be used. This will setup a build process and then run the app in development mode.

```bash
./run.sh
```

### Production

TODO: TBD

## Design decisions

### Represeting integration information for transport, storage

To make events accessible potentially within information sharing ecosystems Riker uses the [Signals](https://github.com/information-sharing-networks/signals) protocol (see simplest signal example).
