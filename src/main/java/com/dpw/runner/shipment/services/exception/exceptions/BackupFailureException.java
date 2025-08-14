package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

/**
    This exception is thrown if the backup process for the V2 0th-day migration fails.
 */
@Generated
public class BackupFailureException extends RuntimeException{

    public BackupFailureException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public BackupFailureException(String msg) {
        super(msg);
    }
}
