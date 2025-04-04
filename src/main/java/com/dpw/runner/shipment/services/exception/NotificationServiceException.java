package com.dpw.runner.shipment.services.exception;

public class NotificationServiceException extends RuntimeException {
    public NotificationServiceException() {
    }

    public NotificationServiceException(String message) {
        super(message);
    }

    public NotificationServiceException(String message, Throwable cause) {
        super(message, cause);
    }
}
