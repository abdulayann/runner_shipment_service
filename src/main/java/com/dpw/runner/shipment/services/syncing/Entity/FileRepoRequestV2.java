package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class FileRepoRequestV2 implements IRunnerRequest {
    @JsonProperty("DocType")
    public String DocType;
    @JsonProperty("FileName")
    public String FileName;
    @JsonProperty("Path")
    public String Path;
    @JsonProperty("InsertUserDisplayName")
    public String InsertUserDisplayName;
    @JsonProperty("ClientEnabled")
    public Boolean ClientEnabled;
    @JsonProperty("isPosted")
    public Boolean isPosted;
    @JsonProperty("MetaData")
    public String MetaData;
    @JsonProperty("IsActive")
    public Integer IsActive;
    @JsonProperty("EventCode")
    public String EventCode;
    //    public String EntityType;
//    public Long EntityId;
    @JsonProperty("Guid")
    private UUID Guid;
}
