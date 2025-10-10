package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.dto.request.orderManagement.AttachDetachOrderRequest;
import com.dpw.runner.shipment.services.dto.request.platform.PurchaseOrdersResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingV3Response;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementDTO;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;
import java.util.Map;

public interface IOrderManagementAdapter {

    ShipmentDetails getOrder(String orderId) throws RunnerException;

    ShipmentDetails getOrderByGuid(String orderGuid) throws RunnerException;

    OrderManagementDTO getOrderManagementDTOByGuid(String orderGuid) throws RunnerException;

    Map<String, OrderManagementDTO> fetchOrdersWithOrderLineAsMap(List<String> orderIds) throws RunnerException;

    void callAttachDetachApi(AttachDetachOrderRequest attachDetachRequest) throws RunnerException;

    List<PurchaseOrdersResponse> getOrdersByShipmentId(String shipmentId) throws RunnerException;

    CustomerBookingResponse getOrderForBooking(String orderId) throws RunnerException;

    CustomerBookingV3Response getOrderForBookingV3(String orderId) throws RunnerException;
}
