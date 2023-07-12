package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "logs")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class Logs extends MultiTenancy {

    @Column(name = "user_name")
    private String userName;

    @Column(name = "action")
    private String action;

    @Column(name = "changed_on")
    private LocalDateTime changedOn;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_name")
    private String entityName;

    @Column(name = "changes")
    private String changes;

    @Column(name = "parent_type")
    private String parentType;

    @Column(name = "parent_id")
    private String parentId;

}
