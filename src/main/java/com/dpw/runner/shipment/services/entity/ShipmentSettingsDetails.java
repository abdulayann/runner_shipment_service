package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "shipment_setting")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentSettingsDetails extends BaseEntity {
    @Column(name = "house_bill_number_lock")
    private Boolean houseBillNumberLock;

    @Column(name = "restrict_hbl_gen")
    private Boolean restrictHblGen;

    @Column(name = "print_phone_number")
    private Boolean printPhoneNumber;

    @Column(name = "house_bill_prefix")
    private String housebillPrefix;

    @Column(name = "house_bill_number_generation")
    private String housebillNumberGeneration;

    @Column(name = "footer_columns")
    private Integer footerColumns;

    @Column(name = "is_auto_populate_ship_type")
    private Boolean isAutoPopulateShipType;

    @Column(name = "partial_cloning_enabled")
    private Boolean partialCloningEnabled;

    @Column(name = "ship_consolidation_container_enabled")
    private Boolean shipConsolidationContainerEnabled;

    @Column(name = "multiple_shipment_enabled")
    private Boolean multipleShipmentEnabled;

    @Column(name = "enable_route_master")
    private Boolean enableRouteMaster;

    @Column(name = "ar_ap_flag")
    private Boolean arApFlag;

    @Column(name = "shipment_ti_Carges_linkage")
    private Boolean shipmentTiCargesLinkage;

    @Column(name = "cooldown_time")
    private Integer cooldownTime;

    @Column(name = "advance_period")
    private Integer advancePeriod;

    @Column(name = "auto_event_create")
    private Boolean autoEventCreate;

    @Column(name = "shipment_lite")
    private Boolean shipmentLite;

    @Column(name = "billing_lite")
    private Boolean billingLite;

    @Column(name = "restricted_locations_enabled")
    private Boolean restrictedLocationsEnabled;

    @Column(name = "is_atd_ata_auto_populate_enabled")
    private Boolean isAtdAtaAutoPopulateEnabled;

    @Column(name = "restricted_locations")
    @ElementCollection
    private List<Integer> restrictedLocations;

    @Column(name = "shipment_import_approver_role")
    private String shipmentImportApproverRole;
}
