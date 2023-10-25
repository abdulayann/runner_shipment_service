package com.dpw.runner.shipment.services.notification.request;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class NotificationMetadata implements Serializable {
    private String from;
    private String subject;
    private NotificationServiceData data;
}
