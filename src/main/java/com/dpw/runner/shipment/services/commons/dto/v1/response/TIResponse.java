package com.dpw.runner.shipment.services.commons.dto.v1.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.TransportInstructionTemplateType;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import java.time.LocalDateTime;

@Data
public class TIResponse implements IRunnerResponse {
    @JsonProperty("Id")
    private Long Id;
    @JsonProperty("WayBillNumber")
    private String WayBillNumber;
    @JsonProperty("TransporterOrgName")
    private String TransporterOrgName;
    @JsonProperty("CarrierBookingOrgName")
    private String CarrierBookingOrgName;
    @JsonProperty("BillingPartyOrgName")
    private String BillingPartyOrgName;
    @JsonProperty("Description")
    private String Description;
    @JsonProperty("TemplateCode")
    @Enumerated(EnumType.ORDINAL)
    private TransportInstructionTemplateType TemplateCode;
    @JsonProperty("FreightModeCode")
    private String FreightModeCode;
    @JsonProperty("TransportRefrenceNumber")
    private String TransportRefrenceNumber;
    @JsonProperty("CarrierServiceLevelCode")
    private String CarrierServiceLevelCode;
    @JsonProperty("ServiceLevelCode")
    private String ServiceLevelCode;
    @JsonProperty("StatusCode")
    private String StatusCode;
    @JsonProperty("RequestedDate")
    private LocalDateTime RequestedDate;
    @JsonProperty("InsertDate")
    private LocalDateTime InsertDate;
}
