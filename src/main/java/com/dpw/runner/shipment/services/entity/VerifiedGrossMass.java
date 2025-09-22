package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.Where;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.util.List;

@Entity
@Table(name = "verified_gross_mass")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE verified_gross_mass SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class VerifiedGrossMass extends MultiTenancy {
    @Column(name = "status", length = 20)
    @Enumerated(value = EnumType.STRING)
    private VerifiedGrossMassStatus status;

    @Column(name = "carrier_bl_no", length = 100)
    private String carrierBlNo;

    @Column(name = "carrier_booking_no", length = 100)
    private String carrierBookingNo;

    @Column(name = "entity_type", length = 50)
    @Enumerated(value = EnumType.STRING)
    private EntityType entityType;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_number", length = 100)
    private String entityNumber;

    @Column(name = "internal_emails", columnDefinition = "TEXT")
    private String internalEmails;

    @Column(name = "external_emails", columnDefinition = "TEXT")
    private String externalEmails;

    @Column(name = "created_by_user_email")
    private String createByUserEmail;

    @Column(name = "submit_by_user_email")
    private String submitByUserEmail;

    @Column(name = "is_delegated")
    private Boolean isDelegated;

    @OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "sailing_information_id", referencedColumnName = "id")
    private SailingInformation sailingInformation;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "requestor_id", referencedColumnName = "id")
    @OrganizationData
    private Parties requestor;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "authorised_id", referencedColumnName = "id")
    @OrganizationData
    private Parties authorised;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "responsible_id", referencedColumnName = "id")
    @OrganizationData
    private Parties responsible;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'VGM_ADDITIONAL_PARTIES'")
    @BatchSize(size = 50)
    private List<Parties> additionalParties;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "verified_gross_mass_id")
    private List<CommonContainers> containersList;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "verified_gross_mass_id")
    private List<ReferenceNumbers> referenceNumbersList;

    @Type(type = "jsonb")
    @Column(name = "submitted_containers_list", columnDefinition = "jsonb")
    private List<CommonContainers> submittedContainersList;
}

