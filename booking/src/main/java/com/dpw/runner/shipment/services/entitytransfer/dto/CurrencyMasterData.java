package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IMasterDataBaseEntity;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CurrencyMasterData implements IMasterDataBaseEntity, Serializable {
    public Long fileId;
    public String CurrenyCode;
    public String CurrenyDescription;
    public Double ExchangeRate;
    public Double BuyRate;
    public LocalDateTime ActivateDate;
    public Double SellRate;
    public String BaseCurrency;
    public Boolean ReciprocalCurrency;
    public Double TransactionRate;
    public Double MarkUpExchangeRate;
    public LocalDateTime DeActivationDate;
}
