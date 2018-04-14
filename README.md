# azure-storage

## Development

### Running tests

- Azure can be mocked with one of the following options:
  - [Azurite](https://github.com/Azure/Azurite): A lightweight server clone of Azure Storage that simulates most of the commands supported by it with minimal dependencies

    ```sh
    $ docker run -d -t -p 10000:10000 -p 10001:10001 -v /tmp/azurite:/opt/azurite/folder arafato/azurite
    ```

  - [Storage Emulator](https://docs.microsoft.com/en-ca/azure/storage/common/storage-use-emulator): The Microsoft Azure storage emulator provides a local environment that emulates the Azure Blob, Queue, and Table services for development purposes.

- Be sure to purge the emulator volume/state directory between test runs! (the tests do not clean up after themselves.)
