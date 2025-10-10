package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.dto.request.billing.RevenueChargeDto;
import lombok.Data;

import java.util.List;

@Data
public class RevenueChargesResponse {
    private Boolean success;
    private String message;
    private List<RevenueChargeDto> data;
}