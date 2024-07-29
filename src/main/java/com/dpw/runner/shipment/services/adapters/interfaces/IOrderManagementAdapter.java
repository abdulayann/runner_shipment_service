package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

public interface IOrderManagementAdapter {

    ShipmentDetails getOrder(String orderId) throws RunnerException;

    CustomerBookingResponse getOrderForBooking(String orderId) throws RunnerException;
}
