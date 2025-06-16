package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ReferenceNumbersModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("CountryOfIssue")
    private String countryOfIssue;
    @JsonProperty("Type")
    private String type;
    @JsonProperty("ReferenceNumber")
    private String referenceNumber;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("BookingId")
    private Long bookingId;
}
