package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;

public class ArrivalNoticeModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public UsersDto usersDto;
    public List<ShipmentContainers> containers;
    public ConsolidationModel consolidationDetails;
    public Hbl hbl;
    public List<ArrivalNoticeBillCharges> arrivalNoticeBillCharges;
    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ArrivalNoticeBillCharges {
        @JsonProperty("ChargeTypeDescription")
        private String ChargeTypeDescription;
        @JsonProperty("ChargeTypeDescriptionLL")
        private String ChargeTypeDescriptionLL;
        @JsonProperty("MeasurementBasis")
        private String MeasurementBasis;
        @JsonProperty("SellAmount")
        private String SellAmount;
        @JsonProperty("TaxAmount")
        private BigDecimal TaxAmount;
        @JsonProperty("BillAmount")
        private BigDecimal BillAmount;
        @JsonProperty("OverseasCurrency")
        private String OverseasCurrency;
    }
}
