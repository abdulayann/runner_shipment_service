package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public class DpsException extends RuntimeException {

    public DpsException() {
    }

    public DpsException(String message) {
        super(message);
    }

    public DpsException(String message, Throwable cause) {
        super(message, cause);
    }

    public DpsException(Throwable cause) {
        super(cause);
    }

    public DpsException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
