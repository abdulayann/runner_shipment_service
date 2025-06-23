package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import javax.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UtilizedLogisticsTransportEquipment implements IRunnerResponse {

  @Size(max = 70, message = "Reference ID must be up to 70 characters.")
  private String referenceId;

  @Size(max = 35, message = "Type of Transport must be up to 35 characters.")
  private String typeOfTransport;

  @Size(max = 9, message = "Size must be up to 9 characters.")
  private String size;

  @Size(max = 35, message = "Seal Number must be up to 35 characters.")
  private String sealNumber;
}

