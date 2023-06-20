package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;

@Entity
@Setter
@Getter
@Table(name = "logs")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class Logs extends BaseEntity {

    @Column(name = "user_id")
    private String userId;

    @Column(name = "user_name")
    private String userName;

    @Column(name = "action")
    private String action;

    @Column(name = "changed_on")
    private String changedOn;

    @Column(name = "table_name")
    private String tableName;

    @Column(name = "row_id")
    private String rowId;

    @Column(name = "module")
    private String module;

    @Column(name = "page")
    private String page;

    @Column(name = "changes")
    private String changes;

    @Column(name = "parent_type")
    private String parentType;

    @Column(name = "parent_id")
    private String parentId;

}
