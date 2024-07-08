package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IOrderManagementAdapter {

    ShipmentDetails getOrder(String orderId) throws RunnerException;

    CustomerBookingResponse getOrderForBooking(String orderId) throws RunnerException;
}
