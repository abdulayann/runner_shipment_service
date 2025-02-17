package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

@Data
public class ArrivalNoticeModel implements IDocumentModel {
    public ShipmentModel shipmentDetails;
    public UsersDto usersDto;
    public ConsolidationModel consolidationDetails;
    public Hbl hbl;
    private List<ShipmentContainers> containers;
    private List<ArrivalNoticeBillCharges> arrivalNoticeBillCharges;

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ArrivalNoticeBillCharges implements Serializable {
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
