package com.dpw.runner.shipment.services.dto.request.platform;

import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementDTO;
import lombok.Data;
import java.util.List;

@Data
public class OrderListResponse {
    private OrderStatus status;
    private int totalPages;
    private int totalRecords;
    private List<OrderManagementDTO> data; // Assuming OrderManagementDTO is existing model for order entries
}
