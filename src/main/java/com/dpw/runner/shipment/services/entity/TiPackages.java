package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;

import javax.persistence.Column;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

public class TiPackages extends MultiTenancy {
    @Column(name = "no_of_packages")
    @Size(max=5, message = "max size is 5 for noOfPackages")
    private String noOfPackages;

    @Column(name = "packageType")
    @MasterData(type = MasterDataType.PACKAGE_TYPE)
    private String packageType;

    @Column(name = "description")
    @Size(max=1024, message = "max size is 1024 for description")
    private String description;

    @Column(name = "dimensions")
    @Size(max=1024, message = "max size is 1024 for description")
    private String dimensions;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    private String grossWeightUnit;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    private String netWeightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    private String volumeUnit;

    @Column(name = "dangerous")
    private Boolean dangerous;

    @Column(name = "substance_name")
    @Size(max=1024, message = "max size is 1024 for substance_name")
    private String substanceName;

    @Column(name = "un_number")
    @Size(max=10, message = "max size is 10 for un_Number")
    private String unNumber;

    @Column(name = "hazard_label")
    @MasterData(type = MasterDataType.DG_CLASS)
    private String hazardLabel;

    @Column(name = "tunnel_restriction_code")
    @Size(max=10, message = "max size is 10 for tunnel_restriction_code")
    private String tunnelRestrictionCode;
}
