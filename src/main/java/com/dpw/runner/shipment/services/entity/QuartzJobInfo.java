package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.JobState;
import com.dpw.runner.shipment.services.entity.enums.JobType;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "quartz_job_info")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuartzJobInfo extends BaseEntity {
    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "tenant_id")
    private Integer tenantId;

    @Enumerated(EnumType.STRING)
    @Column(name = "job_status")
    private JobState jobStatus;

    @Enumerated(EnumType.STRING)
    @Column(name = "job_type")
    private JobType jobType;

    @Column(name = "start_time")
    private LocalDateTime startTime;

    @Column(name = "error_message")
    private String errorMessage;

    @Column(name = "retry_count")
    private Long retryCount;
}
