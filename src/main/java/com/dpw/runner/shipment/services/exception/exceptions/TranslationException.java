package com.dpw.runner.shipment.services.exception.exceptions;

public class TranslationException extends RuntimeException {
    public TranslationException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public TranslationException(String msg) {
        super(msg);
    }
}
