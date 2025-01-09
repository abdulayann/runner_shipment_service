package com.dpw.runner.shipment.services.exception.exceptions;

public class DocumentClientException extends RuntimeException {

    public DocumentClientException() {
    }

    public DocumentClientException(String message) {
        super(message);
    }

    public DocumentClientException(String message, Throwable cause) {
        super(message, cause);
    }
}
