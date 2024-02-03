"use strict"

export function stream(file) {
    return file.stream()
}

async function getReaderImplAsync(stream) {
    let reader = await stream.getReader()
    return reader
}

export const getReaderImpl = stream => () =>
    getReaderImplAsync(stream)

async function readImplAsync(reader, callback) {
    let val = await reader.read().then(chunk => callback(chunk)())
    return val
}

export const readImpl = reader => callback => () =>
    readImplAsync(reader, callback)
