package com.dpw.runner.shipment.services.dto.response.billing;

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
public class BillingDueSummary {
    String branchId;
    String moduleGuid;
    Boolean dueRemaining;
}