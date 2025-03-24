package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AcceptedFileResponse implements IRunnerRequest {
  private String acceptedBranches;
}
