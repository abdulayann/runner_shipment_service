package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "history_meta")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class HistoryDetailMeta extends MultiTenancy {

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "reference_number")
    private String referenceNumber;

    @Column(name = "changed_by_user")
    private String changedByUser;

    @Column(name = "changed_by_user_email")
    private String changedByUserEmail;

    @Column(name = "change_source")
    private String changeSource;

    @Column(name = "change_timestamp")
    private LocalDateTime changeTimestamp;

    @Column(name = "action")
    private String action;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "history_detail_meta_id", referencedColumnName = "id")
    private Set<HistoryDetail> historyDetails = new HashSet<>();
}
