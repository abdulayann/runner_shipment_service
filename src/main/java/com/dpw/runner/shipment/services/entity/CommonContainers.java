package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import jakarta.persistence.*;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "common_containers")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE common_containers SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class CommonContainers extends MultiTenancy {

    @Column(name = "container_code")
    @DedicatedMasterData(type = Constants.CONTAINER_TYPE_MASTER_DATA)
    private String containerCode;

    @Column(name = "count")
    private Long count;

    @Column(name = "goods_description", columnDefinition = "TEXT")
    private String goodsDescription;

    @Column(name = "hs_code")
    private String hsCode;

    @Column(name = "commodity_code")
    @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
    private String commodityCode;

    @Column(name = "commodity_group")
    @MasterData(type = MasterDataType.COMMODITY_GROUP)
    private String commodityGroup;

    @Column(name = "marksn_nums", columnDefinition = "TEXT")
    @Size(max=25000, message = "max size is 25000 for marksn_nums")
    private String marksNums;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;

    @Column(name = "gross_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    @Column(name = "volume_unit", length = 10)
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    @Column(name = "container_no")
    private String containerNo;

    @Column(name = "packs")
    private Integer packs;

    @Column(name = "packs_unit")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsUnit;

    @Column(name = "tare_weight")
    private BigDecimal tareWeight;

    @Column(name = "tare_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String tareWeightUnit;

    @Column(name = "seal_number")
    private String sealNumber;

    @Column(name = "shipper_seal_number")
    @Size(max = 100, message = "max size is 100 for shipper_seal_number")
    private String shipperSealNumber;

    @Column(name = "veterinary_seal_number")
    @Size(max = 100, message = "max size is 100 for veterinary_seal_number")
    private String veterinarySealNumber;

    @Column(name = "customs_seal_number")
    @Size(max = 100, message = "max size is 100 for customs_seal_number")
    private String customsSealNumber;

    @Column(name = "approval_signature")
    private String approvalSignature;

    @Column(name = "approval_date")
    private LocalDateTime approvalDate;

    @Column(name = "vgm_weight")
    private BigDecimal vgmWeight;

    @Column(name = "vgm_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String vgmWeightUnit;

    @Column(name = "weight_determination_method")
    @Enumerated(EnumType.STRING)
    private WeightDeterminationMethodType weightDeterminationMethod;

    @Column(name = "weight_determination_location")
    private String weightDeterminationLocation;

    @Column(name = "weight_determination_datetime")
    private LocalDateTime weightDeterminationDateTime;

    @Column(name = "vgm_status")
    private String vgmStatus;

    @Column(name = "carrier_booking_id")
    private Long carrierBookingId;

    @Column(name = "shipping_instruction_id")
    private Long shippingInstructionId;

    @Column(name = "verified_gross_mass_id")
    private Long verifiedGrossMassId;

    @Column(name = "container_ref_guid")
    private UUID containerRefGuid;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "weighing_party_id", referencedColumnName = "id")
    @OrganizationData
    private Parties weighingParty;
}
