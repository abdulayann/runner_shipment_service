package com.dpw.runner.shipment.services.exception.exceptions;

public class NotificationException extends RuntimeException{
  public NotificationException(String msg) {
    super(msg);
  }

  public NotificationException(Throwable cause) {
    super(cause);
  }
}
