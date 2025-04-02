package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.BatchSize;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Getter
@Setter
@Table(name = "booking_charges")
@AllArgsConstructor
@NoArgsConstructor
public class BookingCharges extends MultiTenancy {
    @Column(name = "booking_id")
    public Long bookingId;

    @Column(name = "seq_no")
    private String seqNo;

    @Column(name = "charges_type")
    @DedicatedMasterData(type = Constants.CHARGE_TYPE_MASTER_DATA)
    private String chargeType; //charge type master data

    @Column(name = "details")
    private String details;

    @Column(name = "charges_code_alt")
    private String chargeCodeAlt;

    @Column(name = "hsn_master")
    private String hsnMaster; //HSN SAC Master data

    @Column(name = "measurement_basis")
    private String measurementBasis; //enum

    @Column(name = "measurement_container_type")
    private String measurementContainerType; //container type master data

    @Column(name = "total_unit_count")
    private BigDecimal totalUnitCount;

    @Column(name = "measurement_unit")
    private String measurementUnit;

    @Column(name = "cost_currency_exchange_update")
    private String costCurrencyExchangeUpdate; //enum

    @Column(name = "reciprocal_currency_cost")
    private Boolean reciprocalCurrencyCost;

    @Column(name = "estimated_cost")
    private BigDecimal estimatedCost;

    @Column(name = "local_cost_amount")
    private BigDecimal localCostAmount;

    @Column(name = "overseas_cost_amount")
    private BigDecimal overseasCostAmount;

    @Column(name = "overseas_cost_currency")
    private String overseasCostCurrency; //currencies master data

    @Column(name = "local_cost_currency")
    private String localCostCurrency; //currencies master data

    @Column(name = "current_cost_rate")
    private BigDecimal currentCostRate;

    @Column(name = "cost_rate_currency")
    private String costRateCurrency; //currencies master data

    @Column(name = "cost_exchange")
    private BigDecimal costExchange;

    @Column(name = "cost_account")
    private String costAccount;

    @Column(name = "cost_comments")
    private String costComments;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "creditor_id", referencedColumnName = "id")
    private Parties creditor;

    @Column(name = "cost_tax_id")
    private String costTaxId;

    @Column(name = "cost_tax_date")
    private LocalDateTime costTaxDate;

    @Column(name = "cost_local_tax")
    private BigDecimal costLocalTax;

    @Column(name = "cost_tax_percentage")
    private BigDecimal costTaxPercentage;

    @Column(name = "cost_overseas_tax")
    private BigDecimal costOverseasTax;

    @Column(name = "cost_no_gst")
    private Boolean costNoGST;

    @Column(name = "cost_tax_type1")
    private BigDecimal costTaxType1;

    @Column(name = "cost_tax_type2")
    private BigDecimal costTaxType2;

    @Column(name = "cost_tax_type3")
    private BigDecimal costTaxType3;

    @Column(name = "cost_tax_type4")
    private BigDecimal costTaxType4;

    @Column(name = "cost_line_total")
    private BigDecimal costLineTotal;

    @Column(name = "sell_currency_exchange_update")
    private String sellCurrencyExchangeUpdate; //enum

    @Column(name = "reciprocal_currency_revenue")
    private Boolean reciprocalCurrencyRevenue;

    @Column(name = "estimated_revenue")
    private BigDecimal estimatedRevenue;

    @Column(name = "local_sell_amount")
    private BigDecimal localSellAmount;

    @Column(name = "overseas_sell_amount")
    private BigDecimal overseasSellAmount;

    @Column(name = "overseas_sell_currency")
    private String overseasSellCurrency; //currencies master data

    @Column(name = "local_sell_currency")
    private String localSellCurrency; //currencies master data

    @Column(name = "current_sell_rate")
    private BigDecimal currentSellRate;

    @Column(name = "sell_rate_currency")
    private String sellRateCurrency; //currencies master data

    @Column(name = "sell_exchange")
    private BigDecimal sellExchange;

    @Column(name = "revenue_account")
    private String revenueAccount;

    @Column(name = "revenue_comments")
    private String revenueComments;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "debtor_id", referencedColumnName = "id")
    private Parties debtor;

    @Column(name = "revenue_tax_id")
    private String revenueTaxId;

    @Column(name = "revenue_tax_date")
    private LocalDateTime revenueTaxDate;

    @Column(name = "local_tax")
    private BigDecimal localTax;

    @Column(name = "tax_percentage")
    private BigDecimal taxPercentage;

    @Column(name = "overseas_tax")
    private BigDecimal overseasTax;

    @Column(name = "no_gst")
    private Boolean noGST;

    @Column(name = "tax_type_1")
    private BigDecimal taxType1;

    @Column(name = "tax_type_2")
    private BigDecimal taxType2;

    @Column(name = "tax_type_3")
    private BigDecimal taxType3;

    @Column(name = "tax_type_4")
    private BigDecimal taxType4;

    @Column(name = "revenue_line_total")
    private BigDecimal revenueLineTotal;

    @Column(name ="internal_remarks")
    private String internalRemarks;

    @Column(name ="external_remarks")
    private String externalRemarks;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "container_charges_mapping",
            joinColumns = {@JoinColumn(name = "charge_id")},
            inverseJoinColumns = {@JoinColumn(name = "container_id")})
    @BatchSize(size = 50)
    private List<Containers> containersList;
}
