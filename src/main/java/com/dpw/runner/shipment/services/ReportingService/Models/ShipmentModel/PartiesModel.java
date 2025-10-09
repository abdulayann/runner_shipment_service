package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
@SuppressWarnings("java:S1948")
public class PartiesModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("EntityType")
    private String entityType;
    @JsonProperty("Type")
    private String type;
    @JsonProperty("OrgCode")
    private String orgCode;
    @JsonProperty("OrgId")
    private String orgId;
    @JsonProperty("TenantId")
    private Integer tenantId;
    @JsonProperty("AddressCode")
    private String addressCode;
    @JsonProperty("OrgData")
    private Map<String, Object> orgData;
    @JsonProperty("AddressData")
    private Map<String, Object> addressData;
    private Boolean isAddressFreeText;
}
