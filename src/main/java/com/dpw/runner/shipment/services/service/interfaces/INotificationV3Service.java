package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.response.NotificationConfirmationMsgResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.apache.http.auth.AuthenticationException;

public interface INotificationV3Service {
    void acceptNotification(Long id);
    NotificationConfirmationMsgResponse confirmationMessage(Long id) throws AuthenticationException, RunnerException;
}