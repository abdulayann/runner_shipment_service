package com.dpw.runner.booking.services.notification.response;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class NotificationServiceResponse implements Serializable {
    private String acknowledgementId;
    private String errorCode;
    private String success;
    private String message;
}
