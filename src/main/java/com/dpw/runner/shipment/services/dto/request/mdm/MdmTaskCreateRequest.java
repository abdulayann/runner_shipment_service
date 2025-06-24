package com.dpw.runner.shipment.services.dto.request.mdm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class MdmTaskCreateRequest implements IRunnerRequest {

  @JsonProperty("status")
  private String status;

  @JsonProperty("roleId")
  private Integer roleId;

  @JsonProperty("entityUuid")
  private String entityUuid;

  @JsonProperty("entityType")
  private String entityType;

  @JsonProperty("isCreatedFromV2")
  private Boolean isCreatedFromV2;

  @JsonProperty("taskType")
  private String taskType;

  @JsonProperty("UserId")
  private Long userId;

  @JsonProperty("sendMail")
  private Boolean sendEmail;
}
