package com.dpw.runner.shipment.services.dto.request.awb;


import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.HeaderContentCode;

import java.math.BigDecimal;
import java.util.List;
import javax.validation.constraints.*;
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
public class AirMessagingAdditionalFields implements IRunnerResponse {
  private Integer densityGroupCode;

  @Size(max = 70, message = "Product ID can have up to 70 alphanumeric characters.")
  private String productId;

  @Pattern(regexp = "^[A-Z]$", message = "Customs Content Code must be a single uppercase letter.")
  private String customsContentCode;

  private String targetCurrencyCode;

  private String marketId;

  private String conversionRate;
  private String conversionId;

  private BigDecimal ccchargesInDestinationCurrency;

  private BigDecimal chargesAtDestination;

  @Pattern(regexp = "^[A-Z]$", message = "Service Type Code must be a single uppercase letter.")
  private String serviceTypeCode;

  private HeaderContentCode headerContentCode;
  private String insuranceCoveringParty;

  private List<UtilizedLogisticsTransportEquipment> utilizedLogisticsTransportEquipment;
  private List<OtherPartyInfo> otherPartyInfo;
}
