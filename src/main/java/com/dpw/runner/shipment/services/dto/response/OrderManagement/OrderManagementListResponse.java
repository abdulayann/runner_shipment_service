package com.dpw.runner.shipment.services.dto.response.OrderManagement;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class OrderManagementListResponse {

    private Object status;
    private List<OrderDataWrapper> data;


    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class OrderDataWrapper {

        private OrderManagementDTO order;
    }

}
