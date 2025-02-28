package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class NotesModel implements Serializable {
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
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime insertDate;
    @JsonProperty("InsertUserDisplayName")
    private String insertUserDisplayName;
    @JsonProperty("IsPublic")
    private Boolean isPublic;
    @JsonProperty("IsActive")
    private Boolean isActive;
}
