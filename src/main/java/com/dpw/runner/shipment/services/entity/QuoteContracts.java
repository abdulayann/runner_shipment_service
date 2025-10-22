package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import org.hibernate.type.SqlTypes;
import java.util.List;

@Entity
@Data
@Table(name = "quote_contracts")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE quote_contracts SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class QuoteContracts extends BaseEntity {

    @Column(name = "contract_id")
    @Size(max=255, message = "max size is 255 for contract_id")
    private String contractId;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "container_types", columnDefinition = "jsonb")
    private List<String> containerTypes;

}
