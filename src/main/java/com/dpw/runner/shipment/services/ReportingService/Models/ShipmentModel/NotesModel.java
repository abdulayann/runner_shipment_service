package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class NotesModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Text")
    private String text;
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("EntityType")
    private String entityType;
    @JsonProperty("InsertUserId")
    private String insertUserId;
    @JsonProperty("InsertDate")
    private LocalDateTime insertDate;
    @JsonProperty("InsertUserDisplayName")
    private String insertUserDisplayName;
    @JsonProperty("IsPublic")
    private Boolean isPublic;
    @JsonProperty("IsActive")
    private Boolean isActive;
}
