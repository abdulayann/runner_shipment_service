package com.dpw.runner.shipment.services.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class QuoteResetRulesResponse {
    private Boolean quotePartyResetFlag;
    private Boolean transportModeResetFlag;
    private Boolean cargoTypeResetFlag;
    private Boolean serviceTypeResetFlag;
    private Boolean originResetFlag;
    private Boolean polResetFlag;
    private Boolean podResetFlag;
    private Boolean destinationResetFlag;
    private Boolean salesBranchResetFlag;
    private Boolean primaryEmailResetFlag;
    private Boolean secondaryEmailResetFlag;
}
