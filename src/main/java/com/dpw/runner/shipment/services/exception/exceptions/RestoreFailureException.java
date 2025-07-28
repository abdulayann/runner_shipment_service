package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

/**
 This exception is thrown if the restore process for the V2 0th-day fails.
 */
@Generated
public class RestoreFailureException extends RuntimeException {

    public RestoreFailureException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public RestoreFailureException(String msg) {
        super(msg);
    }
}
