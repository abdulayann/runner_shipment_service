package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public class GenericException extends RuntimeException {
    public GenericException(String message) {
        super(message);
    }

    public GenericException(String message, Throwable cause) {
        super(message, cause);
    }

    public GenericException(Throwable cause) {
        super(cause);
    }
}
