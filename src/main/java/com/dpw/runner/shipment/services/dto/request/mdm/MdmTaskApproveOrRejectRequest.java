package com.dpw.runner.shipment.services.dto.request.mdm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class MdmTaskApproveOrRejectRequest implements IRunnerRequest {
  @JsonProperty("approvalComments")
  private String approvalComments;

  @JsonProperty("approvedOrRejectedBy")
  private String approvedOrRejectedBy;

  @JsonProperty("taskUuid")
  private String taskUuid;

  @JsonProperty("rejectedComments")
  private String rejectedComments;

  @JsonProperty("status")
  private String status;
}
