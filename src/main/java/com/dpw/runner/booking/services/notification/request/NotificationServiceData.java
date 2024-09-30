package com.dpw.runner.booking.services.notification.request;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class NotificationServiceData implements Serializable {
    private String htmlBody;
}
