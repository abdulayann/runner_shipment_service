package com.dpw.runner.booking.services.adapters.interfaces;

import com.dpw.runner.booking.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;

public interface IOrderManagementAdapter {

    CustomerBookingResponse getOrderForBooking(String orderId) throws RunnerException;
}
