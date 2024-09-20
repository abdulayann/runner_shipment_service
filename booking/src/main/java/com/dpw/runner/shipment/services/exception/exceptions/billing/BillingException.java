package com.dpw.runner.shipment.services.exception.exceptions.billing;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public class BillingException extends RuntimeException {

    public BillingException() {
    }

    public BillingException(String message) {
        super(message);
    }

    public BillingException(String message, Throwable cause) {
        super(message, cause);
    }

    public BillingException(Throwable cause) {
        super(cause);
    }

    public BillingException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
