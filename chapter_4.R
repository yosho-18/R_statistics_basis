# ��4�� ��W�c�ƕW�{

# 4.3 �_����
�g�� <- c(165.2, 175.9, 161.7, 174.2, 172.1, 163.3, 170.9, 170.6, 168.4, 171.3)
mean(�g��)
var(�g��)

# 4.4 ����l���ǂꂭ�炢���Ă͂܂�̂��𒲂ׂ���@
set.seed(1)
�T�C�R��6 <- ceiling(runif(n=6, min=0, max=6))  # runif�F��l���z�ɏ]���f�[�^�𔭐�
table(�T�C�R��6)
set.seed(1)
�T�C�R��600�� <- ceiling(runif(n=6 * 10^6, min=0, max=6))  # runif�F��l���z�ɏ]���f�[�^�𔭐�
table(�T�C�R��600��)
barplot(c(2/3, 1/3), names.arg=c("�j��", "����"))
# 4.4.5 ���K���z
curve(dnorm(x, mean=0, sd=1), from=-4, to=4)
curve(dnorm(x, mean=1, sd=1), add=TRUE)
curve(dnorm(x, mean=0, sd=2), add=TRUE)
dnorm(2, mean=0, sd=1)  # ����0�C�W���΍�1�̊m�����x�֐���x=2�ł̒l
# 4.4.7 ���K��W�c����P������ג��o���s��
�W�{ <- rnorm(n=5, mean=50, sd=10)
�W�{
hist(�W�{)
��W�{ <- rnorm(n=10000, mean=50, sd=10)
��W�{
hist(��W�{)

# 4.5 �W�{���z
# 4.5.3 ���K��W�c�̕ꕽ�ς̐���
�W�{ <- rnorm(n=10, mean=50, sd=10)
�W�{
mean(�W�{)
�W�{ <- rnorm(n=10, mean=50, sd=10)
�W�{
mean(�W�{)
�W�{ <- rnorm(n=10, mean=50, sd=10)
�W�{
mean(�W�{)
# 4.5.4 �W�{���z�����߂�
�W�{���� <- numeric(length=10000)  # ����l���i�[����ꏊ��\��
for(i in 1:10000){  # �����ʂɈ͂܂ꂽ������10000��J��Ԃ��i�����e�J�����V�~�����[�V�����j�C��ԁi10000�܂ށj
	�W�{ <- rnorm(n=10, mean=50, sd=10)  # N(50, 10^2)����n=10�̕W�{�𒊏o
	�W�{����[i] <- mean(�W�{)  # �W�{���ς��v�Z����
}
hist(�W�{����)
�덷��Βl5�ȉ� <- ifelse(abs(�W�{���� - 50) <= 5, 1, 0)
table(�덷��Βl5�ȉ�)
mean(�W�{����)
var(�W�{����)
curve(dnorm(x, mean=50, sd=sqrt(10)), add=TRUE)
dnorm(50, mean=50, sd=sqrt(10))
hist(�W�{����, freq=FALSE)  # �q�X�g�O�����S�̖̂ʐς�1�ɂȂ�悤�ɒ���
curve(dnorm(x, mean=50, sd=sqrt(10)), add=TRUE)
�W�{���� <- numeric(length=10000)  # ����l���i�[����ꏊ��\��
# 4.5.6 �W���덷
for(i in 1:10000){  # �����ʂɈ͂܂ꂽ������10000��J��Ԃ��i�����e�J�����V�~�����[�V�����j�C��ԁi10000�܂ށj
	�W�{ <- rnorm(n=100, mean=50, sd=10)  # N(50, 10^2)����n=100�̕W�{�𒊏o
	�W�{����[i] <- mean(�W�{)  # �W�{���ς��v�Z����
}
var(�W�{����)
hist(�W�{����)

# 4.6 �W�{���ψȊO�̕W�{���z
# 4.6.1 �W�{���U�ƕs�Ε��U�̕W�{���z
�W�{���U <- numeric(length=10000)
�s�Ε��U <- numeric(length=10000)
for(i in 1:10000){
	�W�{ <- rnorm(n=10, mean=50, sd=10)
	�W�{���U[i] <- mean((�W�{ - mean(�W�{))^2)
	�s�Ε��U[i] <- var(�W�{)
}
mean(�W�{���U)
mean(�s�Ε��U)  # ��ʂɁC������̕����ꕪ�U�̒l�ɋ߂��Ȃ�
sd(�W�{���U)
sd(�s�Ε��U)
hist(�W�{���U, breaks=seq(0, 500, 10))
hist(�s�Ε��U, breaks=seq(0, 500, 10))
�W�{���U�덷100�ȏ� <- ifelse(�W�{���U>=200, 1, 0)
�s�Ε��U�덷100�ȏ� <- ifelse(�s�Ε��U>=200, 1, 0)
table(�W�{���U�덷100�ȏ�)
table(�s�Ε��U�덷100�ȏ�)
mean(sqrt(�s�Ε��U))  # �s�ΐ���ʂł͂Ȃ��C�u�s�ΕW���΍��v�Ƃ͌���Ȃ�
# 4.6.2 �����l�̕W�{���z
�W�{���� <- numeric(length=10000)
�W�{�����l <- numeric(length=10000)
for(i in 1:10000){
	�W�{ <- rnorm(n=10, mean=50, sd=10)
	�W�{����[i] <- mean(�W�{)  # ���ς��v�Z
	�W�{�����l[i] <- median(�W�{)  # �����l���v�Z
}
mean(�W�{����)
mean(�W�{�����l)
sd(�W�{����)
sd(�W�{�����l)  # ��ʂɁC�������̂ق����΂�����傫��
hist(�W�{����)
hist(�W�{�����l)

# ���K���
# (1)

# (2)