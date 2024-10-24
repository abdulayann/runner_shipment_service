package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "awb")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class )
@JsonIgnoreProperties(ignoreUnknown = true)
public class Awb extends MultiTenancy {
    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "awb_number")
    private String awbNumber;

    @Type(type = "jsonb")
    @Column(name = "awb_shipment_info", columnDefinition = "jsonb")
    private AwbShipmentInfo awbShipmentInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_notify_party_info", columnDefinition = "jsonb")
    @BatchSize(size = 50)
    private List<AwbNotifyPartyInfo> awbNotifyPartyInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_routing_info", columnDefinition = "jsonb")
    @BatchSize(size = 50)
    private List<AwbRoutingInfo> awbRoutingInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_cargo_info", columnDefinition = "jsonb")
    private AwbCargoInfo awbCargoInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_payment_info", columnDefinition = "jsonb")
    private AwbPaymentInfo awbPaymentInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_other_charges_info", columnDefinition = "jsonb")
    @BatchSize(size = 50)
    private List<AwbOtherChargesInfo> awbOtherChargesInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_other_info", columnDefinition = "jsonb")
    private AwbOtherInfo awbOtherInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_oci_info", columnDefinition = "jsonb")
    @BatchSize(size = 50)
    private List<AwbOCIInfo> awbOciInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_goods_description_info", columnDefinition = "jsonb")
    @BatchSize(size = 50)
    private List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_packing_info", columnDefinition = "jsonb")
    @BatchSize(size = 50)
    private List<AwbPackingInfo> awbPackingInfo;

    @Type(type = "jsonb")
    @Column(name = "awb_special_handling_codes_Mappings", columnDefinition = "jsonb")
    @BatchSize(size = 50)
    private List<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Enumerated(EnumType.STRING)
    @Column(name = "air_message_status")
    private AwbStatus airMessageStatus;

    @Enumerated(EnumType.STRING)
    @Column(name = "linked_hawb_air_message_status")
    private AwbStatus linkedHawbAirMessageStatus;

    @Column(name = "user_mail_id")
    private String userMailId;

    @Column(name = "user_display_name")
    private String userDisplayName;

    @Column(name = "air_message_resubmitted")
    private Boolean airMessageResubmitted;

    @Enumerated(EnumType.STRING)
    @Column(name = "print_type")
    private PrintType printType;

    @Column(name = "original_printed_at")
    private LocalDateTime originalPrintedAt;
}
