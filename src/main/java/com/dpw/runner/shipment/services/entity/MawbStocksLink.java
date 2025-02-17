package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Setter
@Getter
@Table(name = "mawb_stocks_link")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@SQLDelete(sql = "UPDATE mawb_stocks_link SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class MawbStocksLink extends MultiTenancy {
    @Column(name = "parent_id")
    private Long parentId;

    @Column(name = "mawb_number")
    private String mawbNumber;

    @Column(name = "status")
    private String status;

    @Column(name = "seq_number")
    private String seqNumber;

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "ship_cons_number")
    private String shipConsNumber;

//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "parent_id", referencedColumnName = "id")
//    private MawbStocks mawbStocks;
}
