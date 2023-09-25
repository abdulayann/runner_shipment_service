package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class FileRepoModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("FileName")
    private String fileName;
    @JsonProperty("Path")
    private String path;
    @JsonProperty("DocType")
    private String docType;
    @JsonProperty("ClientEnabled")
    private Boolean clientEnabled;
    @JsonProperty("IsPosted")
    private Boolean isPosted;
    @JsonProperty("EventCode")
    private String eventCode;
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("EntityType")
    private String entityType;
}
